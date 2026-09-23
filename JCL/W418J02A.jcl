//W418J02A JOB (640W4180100W418J02A,W100),'RTN W418B4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST8                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* IDDISTR  = &IDDISTR                                                         
//* IDRAPPNR = &IDRAPPNR                                                        
//W418    EXEC W418P02A                                                         
//*                                                                             
//W4182A.SYSIN    DD *                                                          
&IDDISTR,&IDRAPPNR,                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J02A                                         
