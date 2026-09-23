//W233J020 JOB (650W2330100W233J020,W100),'RTN W233V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0,LINES=300                                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W233    EXEC W233P020,INDUT=W233.W233V1                                       
//*                                                                             
//W23320.W23320D3 DD DUMMY                                                      
//W23320.W23320D4 DD DUMMY                                                      
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W233J020                                         
