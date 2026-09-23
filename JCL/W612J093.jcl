//W612J093 JOB (640W6120100W612J093,W100),'RTN W612S5 (6317)',                  
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W612    EXEC W612P093                                                         
//*                                                                             
//W61293.SYSIN DD *                                                             
&A,&B,&C,&D,&E,&F,&G,&H,&I,&J,&K,&L,                                            
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W612J093                                         
