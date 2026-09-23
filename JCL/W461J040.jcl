//W461J040 JOB (650W4610100W461J040,W100),'RTN W461D2',                         
//             USER=?,PASSWORD=?,                                               
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P040,INDIN=W461.W461D2,                                      
//             INDIN2=W476.W461D2,                                              
//             INDUT=W461.W461D2                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W461J040                                         
