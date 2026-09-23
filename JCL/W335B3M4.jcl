//W335B3M4 JOB (670W3350100W335B3M4,W100),'RTN W335B3',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* STARTAR RUTIN B2 SOM BESTÄLLER ERSÄTTNINGSINFO(ARTIKEL)                     
//* FÖR ETT MARKNADSBOLAG M4 = VCSA SOUTH AMERICA                               
//W335     EXEC WSOP                                                            
ORDER W335B3 SYMBOLS                                                            
   VCOM(W335Z3M4)                                                               
END-ORDER                                                                       
