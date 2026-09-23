//W335B2M3 JOB (670W3350100W335B2M3,W100),'RTN W335B2',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* STARTAR RUTIN B2 SOM BESTÄLLER REFRESHING AV ARTIKELINFO                    
//* FÖR ETT MARKNADSBOLAG M3 = ASIA/PACIFIC                                     
//W335     EXEC WSOP                                                            
ORDER W335B2 SYMBOLS                                                            
   VCOM(W335Z1M3) PGM(W33541)                                                   
END-ORDER                                                                       
