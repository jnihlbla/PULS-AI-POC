//W473J090 JOB (650W4730100W473J090,W100),'RTN W473D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
//     INCLUDE MEMBER=SYST0                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W473    EXEC W473P090                                                         
//SOP     EXEC WSOPEND,PROCESS=W473J090                                         
