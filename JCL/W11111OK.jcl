//W11111OK JOB (640W1110100W11111OK,W100),'RTN W111V1',                         
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=9,FORMS=1800,LINECT=0                                           
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> NORMALLY    <== FILEMON-TRANSFER. VCAS -----> VTP         *          
//*                                                                  *          
//*   FILEMON-ÖVERFÖRINGEN FRÅN VCAS TILL VTP  HAR GÅTT BRA  *                  
//*                                                                  *          
//********************************************************************          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W11111FI                                         
