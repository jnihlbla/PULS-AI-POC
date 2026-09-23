//W271J043 JOB (670W2710100W271J043,W100),'RTN W271V6',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W271    EXEC W271P043,                                                        
//             INDIN=W271.W271V6,                                               
//             INDUT=W271.W271V6                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W271J043                                         
