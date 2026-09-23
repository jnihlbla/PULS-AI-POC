//W371J042 JOB (670W3710100W371J042,W100),'RTN W371D1',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST3                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//* OM BMP:N SKA HA KONTROLLKORT ÄNDRA TILL RÄTT NAMN NEDAN                     
//* ANNARS TA BORT KORTET                                                       
/*CNTL XXXXXXXX,EXC                                                             
//W371    EXEC W371P042                                                         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W371J042                                         
