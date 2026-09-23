//W371MOD1 JOB (650W3710100W371MOD1,W100),'RTN W371V9',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//MODIFY   EXEC PROC=WG02MOD,SYSTEM=D2G0,UID=W371MOD1,UTPROC=''                 
MODIFY RECOVERY TABLESPACE DW371.SBYLRAD   DELETE AGE 120                       
MODIFY RECOVERY TABLESPACE DW371.SBYLART   DELETE AGE 120                       
MODIFY RECOVERY TABLESPACE DW371.SBYLKUND  DELETE AGE 120                       
MODIFY RECOVERY TABLESPACE DW371.SBYLDIST  DELETE AGE 120                       
MODIFY RECOVERY TABLESPACE DW371.SBYLACK   DELETE AGE 120                       
RUNSTATS TABLESPACE DW371.SBYLRAD     INDEX(ALL)                                
RUNSTATS TABLESPACE DW371.SBYLART     INDEX(ALL)                                
RUNSTATS TABLESPACE DW371.SBYLKUND    INDEX(ALL)                                
RUNSTATS TABLESPACE DW371.SBYLDIST    INDEX(ALL)                                
RUNSTATS TABLESPACE DW371.SBYLACK     INDEX(ALL)                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W371MOD1                                         
