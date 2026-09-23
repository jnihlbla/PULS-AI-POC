//W23436OK JOB (640W2340100W23436OK,W100),'RTN W234V1',                         
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINES=9,FORMS=1800,LINECT=0                                           
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//********************************************************************          
//*                                                                  *          
//*    ==> NORMALLY    <== FILEMON-TRANSFER. VCAS --> TRUCK PARTS    *          
//*                                                                  *          
//*   FILEMON-ÖVERFÖRINGEN FRÅN VCAS TILL TRUCK PARTS HAR GÅTT BRA   *          
//*                                                                  *          
//********************************************************************          
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W23436FI                                         
