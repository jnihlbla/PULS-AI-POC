//W461J1D5 JOB (650W4610100W461J1D5,W100),'RTN W461V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W461    EXEC W461P0D5,INDIN=W461.W461V1                                       
//SOP     EXEC WSOPEND,PROCESS=W461J1D5                                         
//*                                                                             
