//W335J039 JOB (670W3350100W335J039,W100),'RTN W335B2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W335     EXEC W335P039,                                                       
//             INDIN=W335.W335D5,                                               
//             INDIN2=W335.EMPTY,                                               
//             INDUT2=W335.DUMMY,                                               
//             INDUT=W335.W335B2                                                
//*                                                                             
//W33539.W33539D3 DD MGMTCLAS=DEL2                                              
//W33539.W33539D4 DD MGMTCLAS=DEL2                                              
//W33539.W33539D5 DD MGMTCLAS=DEL2                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J039                                         
