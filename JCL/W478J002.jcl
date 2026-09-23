//W478J002 JOB (650W4780100W478J002,W100),'RTN W478D1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//* DELAY THE EXECUTION TO AVOID ABEND GG                                       
//WAIT    EXEC WWAIT,SECONDS=900                                                
//W478    EXEC W478P002                                                         
//SOP     EXEC WSOPEND,PROCESS=W478J002                                         
