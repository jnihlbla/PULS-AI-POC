//WF10PAL1 JOB (640WF100100WF10PAL1,W100),'RTN WF10D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SQLBTCH EXEC PGM=IKJEFT01,DYNAMNBR=20                                         
//SYSTSPRT DD  SYSOUT=*                                                         
//SYSPRINT DD  SYSOUT=*                                                         
//SYSUDUMP DD  SYSOUT=*                                                         
//SYSTSIN  DD  *                                                                
    DSN SYSTEM(D2G0)                                                            
       RUN PROGRAM(DSNTEP2)  PLAN(DSNTEP2)                                      
    END                                                                         
//SYSIN    DD  DSN=W.QASE.DDL(T01PATLO),DISP=SHR                                
//*                                                                             
//KOLLA EXEC VRCABEND,COND=(4,GE,SQLBTCH)                                       
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=WF10PAL1                                         
