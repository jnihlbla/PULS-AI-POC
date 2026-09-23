//W271J1F1 JOB (670W2710100W271J1F1,W100),'RTN W271V2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W271    EXEC W271P0F1,                                                        
//             INDIN=W271.W271V2                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J1F1                                         
