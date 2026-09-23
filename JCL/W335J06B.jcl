//W335J06B JOB (670W3350100W335J06B,W100),'RTN W335P1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* EMBLEM INFO                                                                 
//*                                                                             
//*   MQ STEP                                                                   
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W335.W335P1.W33546(+0)                                      
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.PARTEMBLEM                                             
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J06B                                         
