//W412S2RS JOB (640W4120100W412S2RS,W100),'RTN W412S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//*  DATE: &ODATE  TIME: &OTIME                                                 
//*  FILE: &OFILE                                                               
//*  MAILID: &MAILID                                                            
//*                                                                             
// EXEC W001HFSG,                                                               
//      PATHIN='&OPATH',                                                        
//      LRECL=500,                                                              
//      DSOUT=W412.W412S2.W41281(+1)                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W412S2RS                                         
