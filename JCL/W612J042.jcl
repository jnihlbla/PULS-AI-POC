//W612J042 JOB (640W6120100W612J042,W100),'RTN W612S4',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST6                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P042                                                         
//*                                                                             
//W61242.W61242D1 DD *                                                          
&U1                                                                             
&U2                                                                             
&U3                                                                             
&U4                                                                             
&U5                                                                             
&U6                                                                             
&U7                                                                             
&U8                                                                             
&U9                                                                             
&U10                                                                            
&U11                                                                            
&U12                                                                            
/*                                                                              
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J042                                         
