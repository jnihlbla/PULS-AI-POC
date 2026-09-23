//W612D3RE JOB (670W6120100W612D3RE,W100),'RTN W612D3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W612D3,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W612D3RE                                         
