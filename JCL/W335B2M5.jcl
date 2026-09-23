//W335B2M5 JOB (670W3350100W335B2M5,W100),'RTN W335B2',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* STARTAR RUTIN B2 SOM BESTÄLLER REFRESHING AV ARTIKELINFO                    
//* FÖR ETT MARKNADSBOLAG M5 = VCNA                                             
//W335     EXEC WSOP                                                            
ORDER W335B2 SYMBOLS                                                            
   VCOM(W335Z1M5) PGM(W33542)                                                   
END-ORDER                                                                       
