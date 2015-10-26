-- THIS FILE IS UNIX ENCODED IF YOU DO NOT SEE NEWLINES CONVERT IT

-- For each country with a population growth of less than 1,
-- return the name of the country along with its population growth
-- (as an XML value).

-- Note: Not sure if the name of the country shall be in the relational
-- output or in the xml output, just in case I'll put it in both.
select name
     , xmlquery( 'let $n := $i/country/name
                  let $g := $i/country/population_growth
                  return <country>
                           {$n}
                           {$g}
                         </country>'
                  passing info as "i"
               ) xml
  from countries
 where 1 > xmlcast( xmlquery( '$i/country/population_growth'
                              passing info as "i"
                            ) as float
                  )
;

-- For each province of each country, return the name of the country,
-- the name of the province and the population density of the province
-- (area divided by population), as normal relational output.

-- Note 1: The bigger name string in the data is "Anatoliki Makedhonia kai
-- Thraki", which is 31 characters long.  varchar(32) shall be enough.

-- Note 2: Erm... population density is actually population/area
-- not area/population.
select c.name
     , t.province
     , ( t.population / (t.area*1.0) ) density
  from countries c
     , xmltable( '$i/country/province'
                 passing c.info as "i"
                 columns province    varchar(32)  path  'name'
                       , area        integer      path  'area'
                       , population  integer      path  'population'
                       ) as t
;

-- For each religion, return the name of the religion along with the
-- number of countries in which that religion is practised
-- (as normal relational output).
select religion
     , count(name) no_of_countries
  from ( select c.name
              , t.religion
           from countries c
              , xmltable( '$i//religions'
                          passing c.info as "i"
                          columns religion varchar(32) path '.' ) as t
       )
 group by religion
 order by no_of_countries desc
;

-- Here's another way to do it, useful if more fields need to be returned
select religion
     , count(name) no_of_countries
  from ( select c.name
              , t.religion
           from countries c
              , xmltable( 'for $r in $i//religions
                           return <root>
                                    <r>{$r}</r>
                                  </root>'
                          passing c.info as "i"
                          columns religion varchar(32) path 'r' ) as t
       )
 group by religion
 order by no_of_countries desc
;

-- For each country, return the name of the country along with an XML attribute
-- value representing information about their cities as follows. The document
-- element is cities. Within this is a city element for each city, with each
-- city element having a name element (for the name of the city) and a province
-- element (for its associated province) as children. The cities element
-- contains a count attribute whose value is the number of cities returned
-- for that country.

-- Note: in the data there are cities that are not inside provinces, these
-- cities are dealt with in the second part of the query and the province field
-- for them is set to the string "NONE".
select name
     , xmlquery( '<cities count="{ count($i//city) }">
                    { for $c in $i/country/province/city
                      return <city>
                               { $c/name }
                               <province>{ string($c/../name) }</province>
                             </city>
                    }
                    { for $c in $i/country/city
                      return <city>
                               { $c/name }
                               <province>NONE</province>
                             </city>
                    }
                  </cities>'
                  passing info as "i"
               ) cities
  from countries
;

