//W479J046 JOB (640W4790100W479J046,W100),'RTN W479M3',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST4                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND D2G0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W479    EXEC W479P046                                                         
//W47946.SYSTSIN DD *                                                           
DSN SYS(D2G0)                                                                   
RUN PROG(W47946) PLAN (W47946)                                                  
END                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W479J046                                         
