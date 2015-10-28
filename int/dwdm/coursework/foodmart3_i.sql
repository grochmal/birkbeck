
select sales_district
     , sales
  from ( select region.sales_district
              , sum(sf_1998.store_sales) sales
              , rank() over (order by sum(sf_1998.store_sales) desc) rank
           from region
              , store
              , ( select store_sales
                       , store_id
                    from sales_fact_1998
                   union all
                  select store_sales
                       , store_id
                    from sales_fact_dec_1998
                ) sf_1998
          where region.region_id = store.region_id
            and store.store_id   = sf_1998.store_id
          group by region.sales_district
       ) sales_by_district
 where rank <= 10
;

