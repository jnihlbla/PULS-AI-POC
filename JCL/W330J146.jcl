//W330J146 JOB (650W3300100W330J146,W100),'RTN W330B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=999,CARDS=0,FORMS=1800,LINECT=0                                 
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYST3                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P146 EXEC W330P146                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J146                                            
