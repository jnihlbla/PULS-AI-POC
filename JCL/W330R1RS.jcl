//W330R1RS JOB (650W3300100W330R1RS,W100),'RTN W330R1',                         
//             CLASS=K,                                                         
//             USER=?,PASSWORD=?                                                
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*+JBS BIND D2G0                                                               
//*                                                                             
//DBSTART  EXEC W001PTSO                                                        
  DSN SYSTEM (D2G0)                                                             
  -START DATABASE (DWFSG1) SPACENAM (*) ACCESS(UT)                              
  END                                                                           
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W330R1RS                                         
