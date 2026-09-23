//W221D6ME JOB (640W2210100W221D6ME,W100),'RTN W221D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0,LINES=999                                         
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*---  WMAILSND,EXEC                                                           
//***********************************************************                   
//*      ÖVERFÖRING LISTA AUTOMATISKA MAIL ARTIKLAR         *                   
//* =>   MAIL       TILL LEVERANTÖRER/ATTENTION PERSON,2119 *                   
//*                                                         *                   
//*   OBS KOLLA FILEN SOM SÄNDS  !   MAIL-ADRESSER          *                   
//***********************************************************                   
//*                                                                             
//EMPTY1 EXEC WEMPTST,DSIN=W221.W221D6.W2212E(+0)                               
//*                                                                             
//    IF (EMPTY1.T.RC NE 4) THEN                                                
//*                                                                             
//MAIL  EXEC WMAILSND,FROM='FROM=HFMATR@VOLVOCARS.COM'                          
//SYSIN DD DSN=W221.W221D6.W2212E(+0),DISP=SHR                                  
//*                                                                             
//    ENDIF                                                                     
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W221D6ME                                         
