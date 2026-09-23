//W011Z1SE JOB (640W0110100W011Z1SE,W100),'RTN W011D1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*                                                                             
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//WQSEN   EXEC WZ11P022,ABSADDRS=CARPARTS.CPAM.SPAREPARTMARKING,                
//             DSIN=W011.W011D1.W0116602(+0)                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W011Z1SE                                         
