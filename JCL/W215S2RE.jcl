//W215S2RE JOB (640W2150100W215S2RE,W100),'RTN W215S2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W215S2,MAXRC=8                                        
//*                                                                             
//*                                                                             
//SOP     EXEC WSOP                                                             
ORDER W416S1                                                                    
//*                                                                             
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W215S2RE                                         
