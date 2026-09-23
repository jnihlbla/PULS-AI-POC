//W461J13F JOB (650W4610100W461J13F,W100),'RTN W461V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P03F,                                                        
//             INDIN=W461.W461V1,                                               
//             INDIN2=W476.W461V1,                                              
//             INDUT=W461.W461V1                                                
//SOP     EXEC WSOPEND,PROCESS=W461J13F                                         
//*                                                                             
