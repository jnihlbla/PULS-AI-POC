//W231J181 JOB (650W2310100W231J181,W100),'RTN W231S1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W231    EXEC W231P181                                                         
//W23181.W23181D3 DD *                                                          
&IDUSER.&URVAL.                                                                 
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W231J181                                         
