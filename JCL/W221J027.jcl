//W221J027 JOB (650W2210100W221J027,W100),'RTN W200V1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
/*JOBPARM LINES=5000                                                            
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=DESTN                                                     
//     INCLUDE MEMBER=SYSTÖ                                                     
//     INCLUDE MEMBER=SYST2                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND VCC1                                                               
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//W221    EXEC W221P027                                                         
//*                                                                             
//* ---- SÄND EXCEL-FILEN TILL BESTÄLLARNA VID OK KÖRNING ---------             
//OK53  IF W221.W22146.RC=0 THEN                                                
//*                                                                             
//*  Beställer nu även jobb W221J127, som skickar den skapade                   
//*  EXCEL-filen 'WUT.W200V1.W22153SP' till SPX-anskaffaren.                    
//SOPORD  EXEC WSOP                                                             
 ORDER W221J127                                                                 
//*                                                                             
//ENDOK53 ENDIF                                                                 
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W221J027                                         
