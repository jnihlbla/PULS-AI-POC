//W092D2OK JOB (540W0920100W092D2OK,W100),'RTN W092D2',                         
//   CLASS=K                                                                    
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//********************************************************************          
//*                                                                  *          
//*   ÖVERFÖRING AV FIL FRÅN  PV/CARPAC Till TRATTEN                 *          
//*   &XTODSN                                                        *          
//*                                                                  *          
//********************************************************************          
//   EXEC PGM=IEFBR14                                                           
//*                                                                             
