//W61161OK JOB (650W6110100W61161OK,W100),'RTN W611S5',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W61161FI                                         
