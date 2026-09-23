//W335B3M5 JOB (670W3350100W335B3M5,W100),'RTN W335B3',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* STARTAR RUTIN B2 SOM BESTÄLLER ERSÄTTNINGSINFO(ARTIKEL)                     
//* FÖR ETT MARKNADSBOLAG M5 = VCNA                                             
//W335     EXEC WSOP                                                            
ORDER W335B3 SYMBOLS                                                            
   VCOM(W335Z3M5)                                                               
END-ORDER                                                                       
