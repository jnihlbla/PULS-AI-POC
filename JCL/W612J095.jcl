//W612J095 JOB (640W6120100W612J095,W100),'RTN W612S6',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P095                                                         
//*                                                                             
//W61295.SYSIN DD *                                                             
&A,&B,&C,&D,&E,&F,&G,&H,&I,&J,&K,&L,                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J095                                         
