//W335J101 JOB (670W3350100W335J101,W100),'RTN W335B1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT NJORS1                                                          
//*KÖR MOT SAMMA PROGRAM SOM J001 I W335V1                                      
//W335    EXEC W335P101                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W335J101                                         
