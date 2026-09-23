//W555Y1RE JOB (650W5550100W555Y1RE,W100),'RTN W555Y1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//FREE    EXEC WFREE,NAME=W555Y1,MAXRC=8                                        
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W555Y1RE                                         
