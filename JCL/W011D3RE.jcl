//W011D3RE JOB (650W0110100W011D3RE,W100),'RTN W011D3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W011D3,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W011D3RE                                         
