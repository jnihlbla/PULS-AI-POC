//W461J120 JOB (650W4610100W461J120,W100),'RTN W461V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W461    EXEC W461P020,DSIN=W462.W461V1.W46247,                                
//             DSIN2=W461.W461V1.W46108,                                        
//             DSIN3=W371.W461V1.W37183(+0),                                    
//             DSIN4=W111.W461V1.W11172,                                        
//             DSIN5=W371.W461V1.W37167(+0),                                    
//             INDUT=W461.W461V1                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W461J120                                         
