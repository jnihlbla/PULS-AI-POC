//W461J020 JOB (650W4610100W461J020,W100),'RTN W461D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P020,DSIN=W461.W461D1.W46112(+0),                            
//             DSIN2=W461.W461D1.W46105(+0),                                    
//             DSIN3=W461.W461D1.W46109(+0),                                    
//             DSIN4=W461.W461D1.W46166(+0),                                    
//             DSIN5=W371.W461D1.W37119                                         
//SOP     EXEC WSOPEND,PROCESS=W461J020                                         
