//W092D2ER JOB (540W0920100W092D2ER,W100),'RTN W092D2',                         
//   CLASS=K                                                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//********************************************************************          
//*                                                                  *          
//*   ÖVERFÖRING AV FIL FRÅN  PV TILL TRATTEN FVB TILL               *          
//*   ANSKAFFNINGEN HAR EJ GÅTT BRA                                  *          
//*   SE I ERROR-LOGGEN (F1XFVC.PROD.ERRLOG) EFTER ORSAKEN.          *          
//*   XFERID: W092D2                                                 *          
//*   FIL:    &XTODSN                                                *          
//*                                                                  *          
//*   OFTA BEHÖVS INGEN ÅTGÄRD. PV UPPTÄCKER OFTA FELET SJÄLVA       *          
//*   OCH SKICKAR OM FILEN.                                          *          
//*                                                                  *          
//********************************************************************          
//*                                                                             
//SOP1    EXEC WSOP,COMMAND='ACTIVATE W092D2ER'                                 
//*                                                                             
//VRCABE  EXEC VRCABEND                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W092D2ER                                         
