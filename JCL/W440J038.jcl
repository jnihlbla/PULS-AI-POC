//W440J038 JOB (670W4400100W440J038,W100),'RTN W440D5',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W440    EXEC W440P038                                                         
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W440S2                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W440J038                                         
