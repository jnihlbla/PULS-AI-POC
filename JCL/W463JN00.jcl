//W463JN00 JOB (640W4630100W463JN00,W100),'RTN W463NY',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//* THIS JOB RUNS AT MIDNIGHT ON NEW YEARS EVE.                                 
//* IT CAUSES THE VCOM RECEPTION JOBS IN THE ROUTINES BELOW                     
//* TO ORDER THE TARGET ROUTINES (S5 AND S9) FOR "TOMORROW"                     
//* (000001), NOT THE NORMAL "TODAY" (000000)                                   
//*                                                                             
//* SEE ALSO JOB W463JN02 WHICH RUNS AT 02:00 ON NEW YEARS DAY.                 
//*                                                                             
//SOP     EXEC WSOP                                                             
 SET VALUE W463X2                                                               
    ON(000001)                                                                  
 END-SET                                                                        
 SET VALUE W463X4                                                               
    ON(000001)                                                                  
 END-SET                                                                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463JN00                                         
