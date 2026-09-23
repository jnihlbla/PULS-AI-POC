//W11450OK JOB (650W1140100W11450OK,W100),'RTN W114S2',                         
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=9,FORMS=1800,LINECT=0                                           
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*   FILEMON-ÖVERFÖRINGEN FRÅN VCAS TILL PV TIKO HAR GÅTT BRA!      *          
//*                                                                  *          
//********************************************************************          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W11450FI                                         
