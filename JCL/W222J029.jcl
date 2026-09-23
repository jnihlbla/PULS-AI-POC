//W222J029 JOB (640W2220100W222J029,W100),'RTN W200V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//W222    EXEC W222P029                                                         
//W22229.SYSIN    DD *                                                          
LOG                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W222J029                                         
