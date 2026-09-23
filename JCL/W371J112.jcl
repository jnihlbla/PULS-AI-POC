//W371J112 JOB (650W3710100W371J020,W100),'RTN W371D8',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
/*JOBPARM LINES=999                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W371    EXEC W371P012,                                                        
//        INDUT=&W371..W371D8                                                   
//W37112.W37112D1 DD DUMMY                                                      
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=W371J112                                         
