//W213D1OK JOB (650W2130100W213D1OK,W100),'RTN W213D1',                         
//         CLASS=K                                                              
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM LINECT=0,FORMS=1800,LINES=50                                          
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*  **************************************************************             
//*  *                                                            *             
//*  *   ==> NORMALLY    <== FILEMON-TRANSFER. INKÖP TILL VCAS    *             
//*  *                                                            *             
//*  *  FILEMON-ÖVERFÖRINGEN FRÅN INKÖP TILL VCAS HAR GÅTT BRA!   *             
//*  *                                                            *             
//*  *  DETTA JOBB BESTÄLLER W213D1 / THIS JOB ORDERS W213D1      *             
//*  *                                                            *             
//*  *                                                            *             
//*  *  OBS   KÖRS NUMERA VIA VCOM                                *             
//*  *                                                            *             
//*  **************************************************************             
//*                                                                             
//*RDS2  EXEC WSOP,COMMAND='ORDER W213D1'                                       
//*                                                                             
//*BE     IF ABEND THEN                                                         
//*OP     EXEC WSOP,COMMAND='ABEND W213D1'                                      
//*BE     ENDIF                                                                 
//*MPTY   EXEC WEMPTST,DSIN=WIN.A130.A13091(+1)                                 
//   EXEC PGM=IEFBR14                                                           
