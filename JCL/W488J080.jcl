//W488J080 JOB (650W4880100W488J080,W100),'RTN W488S3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W488    EXEC W488P080,INDUT=W488.W488S3                                       
//W48880.W48880D2 DD DUMMY                                                      
//SOP     EXEC WSOPEND,PROCESS=W488J080                                         
