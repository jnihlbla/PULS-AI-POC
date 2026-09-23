//W475M1RE JOB (650W4750100W475M1RE,W100),'RTN W475M1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//FREE    EXEC WFREE,NAME=W475M1,MAXRC=8                                        
//*                                                                             
//*UB1    EXEC WFSUBMIT,JCLLIB=W8.IC.JCL,SUBMIT=??????                          
//SOP     EXEC WSOPEND,PROCESS=W475M1RE                                         
