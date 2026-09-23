//W970J041 JOB (640W0000100W970J041,W100),'RTN W970V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST9                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W970    EXEC W970P041,YYWW=&YYWW,                                             
//             YYYY1=&YYYY1,MMDD1=&MMDD1,                                       
//             YYYY2=&YYYY2,MMDD2=&MMDD2,                                       
//             YYYY3=&YYYY3,MMDD3=&MMDD3,                                       
//             YYYY4=&YYYY4,MMDD4=&MMDD4,                                       
//             YYYY5=&YYYY5,MMDD5=&MMDD5,                                       
//             YYYY6=&YYYY6,MMDD6=&MMDD6,                                       
//             YYYY7=&YYYY7,MMDD7=&MMDD7                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W970J041                                         
