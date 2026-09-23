//W092D9RE JOB (650W0920100W092D9RE,W100),'RTN W092D9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W092D9,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W092D9RE                                         
