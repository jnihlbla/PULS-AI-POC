//W118J010 JOB (640W1180100W118J010,W100),'RTN W118D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST1                                                    
/*JOBPARM FORMS=1800,LINECT=0,LINES=9999                                        
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W118    EXEC W118P010                                                         
//W11810.SYSTSIN DD *                                                           
DSN SYS(D2G0)                                                                   
RUN PROG(W11810) PLAN (W11810) LIB('W.QASE.LOAD')                               
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W118J010                                         
