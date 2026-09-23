//W513R1RE JOB (650W5130100W513R1RE,W100),'RTN W513R1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W513R1,MAXRC=8                                        
//SOP     EXEC WSOPEND,PROCESS=W513R1RE                                         
