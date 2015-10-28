
with all_store_sales as
  ( select yearmonth
         , sum(usales) sales
         , store_type
      from ( select to_char(t.the_date, 'YYYYMM') yearmonth
                  , sum(sf.store_sales) usales
                  , s.store_type
               from store s
                  , time_by_day t
                  , ( select store_sales
                           , time_id
                           , store_id
                        from sales_fact_1997
                       union all
                      select store_sales
                           , time_id
                           , store_id
                        from sales_fact_1998
                       union all
                      select store_sales
                           , time_id
                           , store_id
                        from sales_fact_dec_1998
                    ) sf
              where sf.store_id   = s.store_id
                and sf.time_id    = t.time_id
                and s.store_type <> 'HeadQuarters'
              group by s.store_type
                     , to_char(t.the_date, 'YYYYMM')
              union all  -- just in case, generate all months
             select distinct
                    to_char(t.the_date, 'YYYYMM') yearmonth
                  , 0.0 usales
                  , s.store_type
               from time_by_day t
                  , store s
              where s.store_type <> 'HeadQuarters'
           )
     group by yearmonth
            , store_type
  )
select yearmonth
     , acc_sales
     , store_type
  from ( select yearmonth
              , sum(sales) over ( partition by store_type
                                  order by yearmonth
                                  rows 2 preceding
                                ) acc_sales
              , store_type
              from all_store_sales
       )
 where yearmonth >= '199801'
-- where yearmonth >= '199701'
--   and store_type = 'Small Grocery'
 order by yearmonth, store_type
;

