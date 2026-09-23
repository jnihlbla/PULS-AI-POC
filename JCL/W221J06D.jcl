//W221J06D JOB (650W2210200W221J06D,W100),'RTN W221D3',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W221    EXEC W221P06D                                                         
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W221J06D                                         
