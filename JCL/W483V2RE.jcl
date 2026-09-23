//W483V2RE JOB (640W6130100W483V2RE,W100),'RTN W483V2',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST6                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN DD  *                                                                 
  %RENDATE  'W483.W483V2.W48323(+0)' 'W483.V%AAVV..FRAKT' (( PREV               
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W483V2RE                                         
