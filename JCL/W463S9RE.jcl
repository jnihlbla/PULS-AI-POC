//W463S9RE JOB (650W4630100W463S9RE,W100),'RTN W463S9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W463S9                                                
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W463S9RE                                         
