//W418J033 JOB (670W4180100W418J033,W100),'RTN W418S2',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST4                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
//*+JBS BIND IMG0                                                               
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//W418    EXEC W418P033                                                         
//*                                                                             
//SOP     EXEC WSOP                                                             
IF-STATUS WBAT2KRE STARTED                                                      
  END WBAT2KRE                                                                  
END-IF                                                                          
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W418J033                                         
