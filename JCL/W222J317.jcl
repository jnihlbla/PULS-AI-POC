//W222J317 JOB (670W2220100W222J317,W100),'RTN W200V1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND IMG0                                                               
//W222    EXEC W222P017,                                                        
//             INDIN=W222.W200V1                                                
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W222J317                                         
