//W020V1RS JOB (650W0510100W020V1RS,W100),'RTN W020V1',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,CARDS=0                                                    
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//BLOCK   EXEC WBLOCK,NAME=W020V1                                               
//*                                                                             
//*********************************************************************         
//*                                                                             
//*        IDCAMS-DELETE AND IDCAMS-RENAME OF GDG:S                             
//*                                                                             
//*********************************************************************         
//*                                                                             
//RENAME01 EXEC WRTNINP,                                                        
//*                                                                             
//           F1='W225.W225D2.W22508',                                           
//           T1='W225.W020V1.W22508',RF1=FB,LR1=30                              
//*                                                                             
//SOP     EXEC WSOPEND,PROCESS=W020V1RS                                         
//*                                                                             
//SOPINST EXEC WSOP                                                             
IF-CALENDAR BATCH06                                                             
  ACTIVATE W980JPIP SYMBOLS                                                     
    MSG(CA 2 TIMMAR KVAR)                                                       
  END-ACTIVATE                                                                  
END-IF                                                                          
IF-CALENDAR BATCH01                                                             
  ACTIVATE W980JPIP SYMBOLS                                                     
    MSG(CA 2 TIMMAR KVAR)                                                       
  END-ACTIVATE                                                                  
END-IF                                                                          
