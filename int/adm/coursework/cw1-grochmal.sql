-- THIS FILE IS UNIX ENCODED IF YOU DO NOT SEE NEWLINES CONVERT IT
set echo on
set serverout on

/*
 * 1. Write an AFTER trigger that, each time a new row is inserted into table
 * Employee, prints to the standard output the name of the employee and the
 * name of the job that the employee has been assigned to.
 */

create or replace trigger show_employee
after insert on employee
for each row
declare
  jobname job.jbsname%TYPE;
begin
  select jbsname into jobname
    from job
   where job.jbsid = :new.jbsid
  ;
  dbms_output.put_line( 'A new employee: '
                     || :new.empname
                     || ' is working in '
                     || jobname
                     );
end;
/
show errors

/*
 * 2. Write a BEFORE trigger that reacts to the deletion of rows from the
 * Machines table by first deleting the rows from tables Oil and Work that
 * have the same mchID as the rows that are about to be deleted from
 * Machines table.
 */

create or replace trigger kill_machines
before delete on machines
for each row
begin
  delete from work where work.mchid = :old.mchid;
  delete from oil  where  oil.mchid = :old.mchid;
end;
/
show errors

/*
 * 3. Using trigger functionality, ensure that the following conditions
 * are met:
 *   - When an employee's contribution to a project is deleted, the cost of the
 *     project is reduced by 10%.
 *   - When an employee is deleted, all the contributions by that employee to
 *     all projects are dropped.
 */

create or replace trigger contribution
after delete on empprjcontribution
for each row
declare
  project_cost proj.prjcost%TYPE;
begin
  select prjcost into project_cost
    from proj
   where proj.prjid = :old.prjid
  ;
  project_cost := 0.9 * project_cost;
  update proj
     set prjcost = project_cost
   where prjid = :old.prjid
  ;
end;
/
show errors

create or replace trigger dismiss
before delete on employee
for each row
begin
  delete from empprjcontribution
   where empid = :old.empid
  ;
end;
/
show errors

/*
 * 4. Write a BEFORE trigger that reacts to the update of the wHours
 * attribute of any row in the Work table by checking if the new wHours value
 * exceeds the old value by more than 5, in which case an appropriate message
 * should be output and the old value of wHours should be reinstated.
 */

create or replace trigger too_many_hours
before update of whours on work
for each row
-- could also be done using a when clause instead of the if
--when (abs(new.whours - old.whours) > 5)
begin
  if (abs(:new.whours - :old.whours) > 5) then
    :new.whours := :old.whours;
    dbms_output.put_line( 'Update on work ID '
                       || :old.wID
                       || ' aborted.  A change in hours cannot exceed 5.'
                       );
  end if;
end;
/
show errors

/*
 * 1. Add the following family members to family rdf data:
 *   - Frank
 *   - Emma as child of Cathy and Frank
 */

insert into family_rdf_data values
  ( 36
  , sdo_rdf_triple_s('grochmal'
    , 'http://www.example.org/family/Frank'
    , 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
    , 'http://www.example.org/family/Male'
    )
  )
;

insert into family_rdf_data values
  ( 37
  , sdo_rdf_triple_s('grochmal'
    , 'http://www.example.org/family/Frank'
    , 'http://www.example.org/family/fatherOf'
    , 'http://www.example.org/family/Emma'
    )
  )
;

insert into family_rdf_data values
  ( 38
  , sdo_rdf_triple_s('grochmal'
    , 'http://www.example.org/family/Cathy'
    , 'http://www.example.org/family/motherOf'
    , 'http://www.example.org/family/Emma'
    )
  )
;

-- 2. Create a new rulebase, family auntie containing this rule.

execute sem_apis.create_rulebase('grochmal_auntie_rb');

/*
 * 3. Create a rule that allows aunties to be determined by inferencing.
 * Note: for the purpose of this exercise an auntie is a family
 * relationship between a person and his or her parent's sister
 * (i.e. blood aunts only).
 */

insert into mdsys.semr_grochmal_auntie_rb values
  ( 'auntie_rule'
  , '(?x :sisterOf ?y) (?y :parentOf ?z)'
  , null
  , '(?x :auntieOf ?z)'
  , sem_aliases(sem_alias('','http://www.example.org/family/'))
  )
;

-- 4. Create the entailment (rules index) for the rulebase family auntie

begin
  sem_apis.create_entailment
    ( 'rdfs_auntie_grochmal'
    , sem_models('grochmal')
    , sem_rulebases('RDFS','grochmal_auntie_rb')
    , sem_apis.reach_closure
    , null
    , 'USER_RULES=T'
    )
  ;
end;
/
show errors

-- 5. Write a query to determine the aunties of each person in the family.

select '--- without inferencing ---' from dual;
select x auntie , y child
  from table(sem_match
    ( '(?x :auntieOf ?y)'
    , sem_models('grochmal')
    , null
    , sem_aliases(sem_alias('','http://www.example.org/family/'))
    , null
    )
  )
;

select '--- with inferencing ---' from dual;
select x auntie , y child
  from table(sem_match
    ( '(?x :auntieOf ?y)'
    , sem_models('grochmal')
    , sdo_rdf_rulebases('RDFS','grochmal_auntie_rb')
    , sem_aliases(sem_alias('','http://www.example.org/family/'))
    , null
    )
  )
;

-- cleanup (commented out in the coursework)
--execute sem_apis.drop_entailment('rdfs_auntie_grochmal');
--execute sem_apis.drop_rulebase('grochmal_auntie_rb');
--delete from family_rdf_data where id in (36,37,38);

-- 6. Run the query and obtain the results with and without inferencing.

-- there are no results without inferencing:
/*
 * AUNTIE  CHILD
 * -------------
 */

-- results with inferencing:
/*
 * AUNTIE                               CHILD
 * ------------------------------------------------------------------------
 * http://www.example.org/family/Suzie  http://www.example.org/family/Cindy
 * http://www.example.org/family/Suzie  http://www.example.org/family/Tom
 */

