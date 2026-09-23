//W611J111 JOB (670W6110100W611J111,W100),'RTN W611S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYSTÖ                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* OM BMP:N SKA HA KONTROLLKORT ÄNDRA TILL RÄTT NAMN NEDAN                     
//* ANNARS TA BORT KORTET                                                       
//W611    EXEC W611P011                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W611J111                                         
