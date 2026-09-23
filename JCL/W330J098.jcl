//W330J098 JOB (650W3300100W330J098,W100),'RTN W330Y1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K,TIME=(10,0)                                              
/*JOBPARM LINES=999,CARDS=0,FORMS=1800,LINECT=0                                 
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST0                                                     
//     INCLUDE MEMBER=SYST3                                                     
//     INCLUDE MEMBER=SYST9                                                     
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W330P098 EXEC W330P098                                                        
//*                                                                             
//END  EXEC WSOPEND,PROCESS=W330J098                                            
