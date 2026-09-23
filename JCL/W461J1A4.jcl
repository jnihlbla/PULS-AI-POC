//W461J1A4 JOB (650W4610100W461J1A4,W100),'RTN W461V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
/*JOBPARM LINES=999                                                             
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*CNTL  W461LOG,EXC                                                             
//W461    EXEC W461P0A4,INDIN=W461.W461V1                                       
//SOP     EXEC WSOPEND,PROCESS=W461J1A4                                         
//*                                                                             
