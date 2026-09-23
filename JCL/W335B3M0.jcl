//W335B3M0 JOB (670W3350100W335B3M0,W100),'RTN W335B3',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//* STARTAR RUTIN B2 SOM BESTÄLLER ERSÄTTNINGSINFO(ARTIKEL)                     
//* FÖR ETT MARKNADSBOLAG M0 = VCAS                                             
//W335     EXEC WSOP                                                            
ORDER W335B3 SYMBOLS                                                            
   VCOM(W335Z3M0)                                                               
END-ORDER                                                                       
